// Use ./compile.sh to compile this file
// See comments there for further information...

//#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))


typedef struct {
  float x,y,z;
} Vec3;


typedef struct {
  Vec3    *data;
  uint32_t size;
  uint32_t idx;
  uint32_t used;
} trail_t;


typedef struct {
  Vec3  pos;  // 12 bytes, 3 floats
  Vec3  vel;  // 12 bytes, 3 floats
  Vec3  acc;  // 12 bytes, 3 floats
  float dist; // 4 bytes,  1 float
} Agent; // 40 bytes, 10 floats

typedef struct {
  Agent *agents;        // 4 bytes
  Vec3 *agents_pos;
  Vec3 *agents_vel;
  Vec3 *agents_acc;
  float *agents_dist;
  uint32_t agent_count; // 4
  float speed;          // 4
  float cohesion;       // 4
  float separation;     // 4
  float alignment;      // 4
  float radius;         // 4
  Vec3  size;           // 12
  trail_t *trail;       // 4
} AgentSystem;



static void create_trail(AgentSystem *sys, uint32_t max_trail) {
  trail_t *trail = (trail_t *)malloc(sizeof(trail_t));
  trail->idx = 0;
  trail->used = 0;
  trail->size = 0;
  trail->data = (Vec3 *)malloc(max_trail*sizeof(Vec3));;
  sys->trail = trail;
}

static void destroy_trail(AgentSystem *sys) {
  free(sys->trail->data);
  free(sys->trail);
}


static void update_trail_size(trail_t *trail, uint32_t size) {
  trail->size = size;
}


static inline void into_trail(trail_t *trail, Vec3 *p) {
  trail->idx %= trail->size;
  trail->data[trail->idx] = *p;
  trail->idx++;
  if(trail->used < trail->size) {
    trail->used++;
  } else {
    trail->used = trail->size;
  }
}


static inline void memcpy_trail(trail_t *trail, Vec3 *p, uint32_t size) {
  uint32_t count = 0;
  uint32_t chunk = 0;

  while(count < size) {
    trail->idx %= trail->size;
    chunk = MIN(trail->size - trail->idx, size - count);
    memcpy(&trail->data[trail->idx], &p[count], chunk*sizeof(Vec3));
    count+=chunk;
    trail->idx+=chunk;
  }
  trail->used += size;
  if(trail->used > trail->size) {
    trail->used = trail->size;
  }
}



static inline void setVec3(Vec3 *v, float x, float y, float z) {
  v->x = x;
  v->y = y;
  v->z = z;
}


static inline void copyVec3(Vec3* v, Vec3* v2) {
  v->x = v2->x;
  v->y = v2->y;
  v->z = v2->z;
}



static inline void addVec3(Vec3* v, Vec3* v2) {
  v->x += v2->x;
  v->y += v2->y;
  v->z += v2->z;
}


static inline void subVec3(Vec3* v, Vec3* v2) {
  v->x -= v2->x;
  v->y -= v2->y;
  v->z -= v2->z;
}


static inline void scaleVec3(Vec3* v, float s) {
  v->x *= s;
  v->y *= s;
  v->z *= s;
}


static inline void normalizeVec3(Vec3* v, float l) {
  float m = sqrtf(v->x * v->x + v->y * v->y + v->z * v->z);
  if (m > 0.0f) {
    l /= m;
    v->x *= l;
    v->y *= l;
    v->z *= l;
  }
}

static inline float distVec3(Vec3 *v, Vec3 *v2) {
  float dx = v->x - v2->x;
  float dy = v->y - v2->y;
  float dz = v->z - v2->z;
  return sqrtf(dx * dx + dy * dy + dz * dz);
}

static inline float randf01() {
  return (float)rand() / (float)RAND_MAX;
}


static inline float randf() {
  return randf01() * 2.0f - 1.0f;
}


static void init_agents(AgentSystem *sys, uint32_t start, uint32_t end) {
    for(uint32_t i = start; i < end; i++) {

      setVec3(&sys->agents_pos[i], randf01()*sys->size.x,
	      randf01()*sys->size.y, randf01()*sys->size.z);
    
      setVec3(&sys->agents_vel[i], randf()*sys->speed,
	      randf()*sys->speed, randf()*sys->speed);

      setVec3(&sys->agents_acc[i], 0, 0, 0);

      sys->agents_dist[i] = 0;
    }
}


static void update_agent_count(AgentSystem *sys, uint32_t count) {
  if(sys->agent_count < count) {
    init_agents(sys, sys->agent_count, count);
  }
  sys->agent_count = count;
}



static void create_agents(AgentSystem *sys, uint32_t max_count) {
  sys->agents_pos = (Vec3 *)malloc(max_count*sizeof(Vec3));
  sys->agents_vel = (Vec3 *)malloc(max_count*sizeof(Vec3));
  sys->agents_acc = (Vec3 *)malloc(max_count*sizeof(Vec3));
  sys->agents_dist = (float *)malloc(max_count*sizeof(float));
  
  sys->agent_count = 0;
}



static void move(AgentSystem *sys) {
  for(uint32_t i=0; i < sys->agent_count; i++) {
    addVec3(&sys->agents_vel[i], &sys->agents_acc[i]);
    normalizeVec3(&sys->agents_vel[i], sys->speed);
    addVec3(&sys->agents_pos[i], &sys->agents_vel[i]);
    setVec3(&sys->agents_acc[i],0,0,0);
  }
  memcpy_trail(sys->trail, sys->agents_pos, sys->agent_count);
}



static void bounce(AgentSystem *sys) {
  for(uint32_t i=0; i < sys->agent_count; i++) {
    if(((sys->agents_pos[i].x < 0) && (sys->agents_vel[i].x < 0)) ||
       ((sys->agents_pos[i].x > sys->size.x) && (sys->agents_vel[i].x > 0))) {
      sys->agents_vel[i].x *= -1.0f;
    }

    if(((sys->agents_pos[i].y < 0) && (sys->agents_vel[i].y < 0)) ||
       ((sys->agents_pos[i].y > sys->size.y) && (sys->agents_vel[i].y > 0))) {
      sys->agents_vel[i].y *= -1.0f;
    }

    if(((sys->agents_pos[i].z < 0) && (sys->agents_vel[i].z < 0)) ||
       ((sys->agents_pos[i].z > sys->size.z) && (sys->agents_vel[i].z > 0))) {
      sys->agents_vel[i].z *= -1.0f;
    }
  }
}


 static void swarm_separate(AgentSystem *sys, uint32_t idx, Vec3 *result) {
   Vec3 tmp, force;

   setVec3(&force,0,0,0);
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     if(sys->agents_dist[i]) {
       copyVec3(&tmp, &sys->agents_pos[idx]);
       subVec3(&tmp, &sys->agents_pos[i]);
       scaleVec3(&tmp, sys->radius / sys->agents_dist[i]);
       addVec3(&force, &tmp);
     }
   }

   normalizeVec3(&force, 1.0f);
   scaleVec3(&force, sys->separation);
   addVec3(result, &force);
 }


 static void swarm_align(AgentSystem *sys, uint32_t idx, Vec3 *result) {
   static Vec3 tmp, force;
   uint32_t near = 0;

   setVec3(&force,0,0,0);
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     if(sys->agents_dist[i]) {
       copyVec3(&tmp, &sys->agents_vel[i]);
       scaleVec3(&tmp, sys->radius / sys->agents_dist[i]);
       addVec3(&force, &tmp);
       near++;
     }
   }

   if(near) {
     scaleVec3(&force, 1.0f / near);
     normalizeVec3(&force, 1.0f);
     scaleVec3(&force, sys->alignment);
     addVec3(result, &force);
   }
 }


 static void swarm_cohere(AgentSystem *sys, uint32_t idx, Vec3 *result) {
   static Vec3 force;
   uint32_t near = 0;
   float dist;

   setVec3(&force,0,0,0);

   for(uint32_t i=0; i < sys->agent_count; i++) {
     if(sys->agents_dist[i]) {
       addVec3(&force, &sys->agents_pos[i]);
       near++;
     }
   }

   if(near) {
     scaleVec3(&force, 1.0f / near);
     dist = distVec3(&sys->agents_pos[idx], &force);
     if(dist) {
       subVec3(&force, &sys->agents_pos[idx]);
       scaleVec3(&force, sys->radius / dist);
       normalizeVec3(&force, 1.0f);
       scaleVec3(&force, sys->cohesion);
       addVec3(result, &force);
     }
   }
 }


 static int swarm_distance(AgentSystem *sys, uint32_t idx) {
   int count = 0;
   float dist;

   for(uint32_t i=0; i < sys->agent_count; i++) {
     sys->agents_dist[i] = 0;
     dist = distVec3(&sys->agents_pos[idx], &sys->agents_pos[i]);
     if(dist && (dist < sys->radius)) {
       sys->agents_dist[i] = dist;
       count++;
     }
   }
   return count;
 }



 static void swarm(AgentSystem *sys) {
   Vec3 force;
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     if(swarm_distance(sys,i)) {
       setVec3(&force,0,0,0);
       swarm_separate(sys, i, &force);
       swarm_align(sys, i, &force);
       swarm_cohere(sys, i, &force);
       addVec3(&sys->agents_acc[i], &force);
     }
   }
 }


uint32_t get_agent_count(AgentSystem *sys) {
  return sys->agent_count;
}

Agent* get_agents_pointer(AgentSystem *sys) {
  return sys->agents;
}

Vec3* get_trail_pointer(AgentSystem *sys) {
  return sys->trail->data;
}

uint32_t get_trail_size(AgentSystem *sys) {
  return sys->trail->used;
}


float get_agent_component(AgentSystem* sys, uint32_t idx, uint32_t component) {
  float *pos = (float*)&sys->agents_pos[idx];
  return pos[component];
}


EMSCRIPTEN_KEEPALIVE AgentSystem* update_agent_config(AgentSystem *sys, float x, float y, float z, uint32_t count, float cohesion,
						      float separation, float alignment, float speed, float radius, float trail_size) {

   setVec3(&sys->size, x, y, z);
   sys->separation = separation;
   sys->cohesion = cohesion;
   sys->alignment = alignment;
   sys->speed = speed;
   sys->radius = radius;
   update_agent_count(sys, count);
   update_trail_size(sys->trail, (uint32_t)(trail_size*count));
   
   return sys;
}


EMSCRIPTEN_KEEPALIVE AgentSystem* init_agent_system(float x, float y, float z, uint32_t count, uint32_t max_agents, float cohesion,
						    float separation, float alignment, float speed, float radius, float trail_size, uint32_t max_trail) {
  AgentSystem *sys = (AgentSystem*)malloc(sizeof(AgentSystem));
  create_trail(sys, max_trail*max_agents);
  create_agents(sys, max_agents);
  update_agent_config(sys, x, y, z, count, cohesion, separation, alignment, speed, radius, trail_size);
  memcpy_trail(sys->trail, sys->agents_pos, sys->agent_count);

  return sys;
}


void destroy_agent_system(AgentSystem* sys) {
  destroy_trail(sys);
  free(sys->agents_pos);
  free(sys->agents_vel);
  free(sys->agents_acc);
  free(sys->agents_dist);
  free(sys);
}


EMSCRIPTEN_KEEPALIVE AgentSystem* update_agent_system(AgentSystem* sys) {
  move(sys);
  swarm(sys);
  bounce(sys);
  return sys;
}

