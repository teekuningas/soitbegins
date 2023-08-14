#!/bin/sh

# Trap signals and forward them
trap_handler() {
    echo "Received a signal, propagating to Nginx..."
    kill -s "$1" "$nginx_pid"
}

trap 'trap_handler TERM' TERM;
trap 'trap_handler INT' INT;
trap 'trap_handler QUIT' QUIT;
trap 'trap_handler HUP' HUP;

echo "Replacing env variables in JS files"
for file in /app/dist/index.*.js; do
  if [ -f "$file" ]; then
    sed -i "s|%%RUNTIME_SERVER_UPDATE_INTERVAL%%|${SERVER_UPDATE_INTERVAL:-1000}|g" $file
    sed -i "s|%%RUNTIME_SERVER_API%%|${SERVER_API:-ws://localhost:8765}|g" $file
    sed -i "s|%%RUNTIME_MODEL_EARTH%%|${MODEL_EARTH:-http://localhost:9000/earth.zip}|g" $file
  fi
done

echo "Starting to serve at 9000"
nginx -g "daemon off;" &
nginx_pid=$!
wait "$nginx_pid"
