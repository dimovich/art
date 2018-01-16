// Use ./compile.sh to compile this file
// See comments there for further information...

//#include <stdio.h>
#include <stdint.h>
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
  Vec3  pos;  // 12 bytes, 3 floats
  Vec3  vel;  // 12 bytes, 3 floats
  Vec3  acc;  // 12 bytes, 3 floats
  float dist; // 4 bytes,  1 float
} Agent; // 40 bytes, 10 floats

typedef struct {
  Agent *agents;        // 4 bytes
  uint32_t agent_count; // 4
  float speed;          // 4
  float cohesion;       // 4
  float separation;     // 4
  float alignment;      // 4
  float radius;         // 4
  Vec3  size;           // 12
} AgentSystem;


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


static Agent* generate_agents(Vec3 size, float speed, uint32_t agent_count) {
  Agent *agents = (Agent*)malloc(agent_count * sizeof(Agent));
  Agent *a;

  for(uint32_t i=0; i < agent_count; i++) {
    a = &agents[i];
    setVec3(&a->pos, randf01()*size.x, randf01()*size.y, randf01()*size.z);
    setVec3(&a->vel, randf()*speed, randf()*speed, randf()*speed);
    setVec3(&a->acc, 0, 0, 0);
    a->dist = 0;
  }

  return agents;
}



static inline void limitVec3(Vec3 *v, float l) {
  float *v2 = (float *)v;
  float ratio = 0.0f;
  for(uint32_t i = 0; i<3; i++ ) {
    if(v2[i] > l) {
      ratio = l / v2[i];
      for(uint32_t j = 0; j<3; j++ ) {
	v2[j] *= ratio;
      }
    }
  }
}


static void move(AgentSystem *sys) {
  Agent *a;
  Vec3 force;

  for(uint32_t i=0; i < sys->agent_count; i++) {
    a = &sys->agents[i];
    copyVec3(&force, &a->vel);
    addVec3(&force, &a->acc);
    normalizeVec3(&force, sys->speed);
    copyVec3(&a->vel,&force);
    addVec3(&a->pos, &a->vel);
    setVec3(&a->acc,0,0,0);
  }
}


 
static void bounce(AgentSystem *sys) {
  Agent *a;

  for(uint32_t i=0; i < sys->agent_count; i++) {
    a = &sys->agents[i];
    
    if(((a->pos.x < 0) && (a->vel.x < 0)) ||
       ((a->pos.x > sys->size.x) && (a->vel.x > 0))) {
      a->vel.x *= -1.0f;
    }

    if(((a->pos.y < 0) && (a->vel.y < 0)) ||
       ((a->pos.y > sys->size.y) && (a->vel.y > 0))) {
      a->vel.y *= -1.0f;
    }

    if(((a->pos.z < 0) && (a->vel.z < 0)) ||
       ((a->pos.z > sys->size.z) && (a->vel.z > 0))) {
      a->vel.z *= -1.0f;
    }
  }
}



 static void swarm_separate(AgentSystem *sys, Agent *a, Vec3 *result) {
   Vec3 tmp, force;
   Agent *other;

   setVec3(&force,0,0,0);
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     other = &sys->agents[i];
     
     if(other->dist) {
       copyVec3(&tmp, &a->pos);
       subVec3(&tmp, &other->pos);
       scaleVec3(&tmp, sys->radius / other->dist);
       addVec3(&force, &tmp);
     }
   }

   normalizeVec3(&force, 1.0f);
   scaleVec3(&force, sys->separation);
   addVec3(result, &force);
 }



 static void swarm_align(AgentSystem *sys, Agent *a, Vec3 *result) {
   Vec3 tmp, force;
   Agent *other;
   uint32_t near = 0;

   setVec3(&force,0,0,0);
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     other = &sys->agents[i];
     
     if(other->dist) {
       copyVec3(&tmp, &other->vel);
       scaleVec3(&tmp, sys->radius / other->dist);
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



 static void swarm_cohere(AgentSystem *sys, Agent *a, Vec3 *result) {
   Vec3 tmp, force;
   Agent *other;
   uint32_t near = 0;
   float dist;

   setVec3(&force,0,0,0);

   for(uint32_t i=0; i < sys->agent_count; i++) {
     other = &sys->agents[i];
     if(other->dist) {
       addVec3(&force, &other->pos);
       near++;
     }
   }

   if(near) {
     scaleVec3(&force, 1.0f / near);
     dist = distVec3(&a->pos, &force);
     if(dist) {
       subVec3(&force, &a->pos);
       scaleVec3(&force, sys->radius / dist);
       normalizeVec3(&force, 1.0f);
       scaleVec3(&force, sys->cohesion);
       addVec3(result, &force);
     }
   }
 }


 static int swarm_distance(AgentSystem *sys, Agent *a) {
   Agent *other;
   int count = 0;
   float dist;

   for(uint32_t i=0; i < sys->agent_count; i++) {
     other = &sys->agents[i];
     other->dist = 0;
     dist = distVec3(&a->pos, &other->pos);
     if(dist && (dist < sys->radius)) {
       other->dist = dist;
       count++;
     }
   }
   return count;
 }


 static void swarm(AgentSystem *sys) {
   Vec3 force;
   Agent *a;
   
   for(uint32_t i=0; i < sys->agent_count; i++) {
     a = &sys->agents[i];

     if(swarm_distance(sys,a)) {
       setVec3(&force,0,0,0);
       swarm_separate(sys, a, &force);
       swarm_align(sys, a, &force);
       swarm_cohere(sys, a, &force);
       addVec3(&a->acc, &force);
     }
   }
 }


uint32_t get_agent_count(AgentSystem* sys) {
  return sys->agent_count;
}

Agent* get_agents_pointer(AgentSystem* sys) {
  return sys->agents;
}


float get_agent_component(AgentSystem* sys, uint32_t idx, uint32_t component) {
  float *pos = (float*)&(sys->agents[idx]).pos;
  return pos[component];
}


EMSCRIPTEN_KEEPALIVE AgentSystem* update_agent_config(AgentSystem *sys, float x, float y, float z, uint32_t count, float cohesion,
						      float separation, float alignment, float speed, float radius) {

   setVec3(&sys->size, x, y, z);
   sys->agent_count = count;
   sys->separation = separation;
   sys->cohesion = cohesion;
   sys->alignment = alignment;
   sys->speed = speed;
   sys->radius = radius;
   
   return sys;
}


 EMSCRIPTEN_KEEPALIVE AgentSystem* init_agent_system(float x, float y, float z, uint32_t count, float cohesion,
						     float separation, float alignment, float speed, float radius) {
  AgentSystem *sys = (AgentSystem*)malloc(sizeof(AgentSystem));

  update_agent_config(sys, x, y, z, count, cohesion, separation, alignment, speed, radius);
  sys->agents = generate_agents(sys->size, sys->speed, sys->agent_count);

  return sys;
}



EMSCRIPTEN_KEEPALIVE AgentSystem* update_agent_system(AgentSystem* sys) {
  
  move(sys);
  swarm(sys);
  bounce(sys);
  
  return sys;
}

