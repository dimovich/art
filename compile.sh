#!/bin/sh

# This compiles the file agents.c with optimizations,
# declares a number of exported function names,
# post-processes the resulting JS with Closure compiler and
# wraps all within a global 'Agents' module/object
# This module MUST be initialized by calling 'Agents();'

emcc -O2 -s ASM_JS=1 -s INVOKE_RUN=0 \
     -s EXPORTED_FUNCTIONS="['_main','_init_agent_system','_update_agent_system','_get_agent_count','_get_agent_component','_get_agents_pointer','_update_agent_config']" \
     -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' \
     -s "EXPORT_NAME='Agents'" \
     -s MODULARIZE=1 \
     --memory-init-file 0 \
     --closure 1 \
     -o resources/js/agents.js \
     agents.c

