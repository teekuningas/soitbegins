.PHONY: help dev frontend server simulation shared clean reorganize

# Default target
help:
	@echo "So It Begins - Root Makefile"
	@echo ""
	@echo "Available commands:"
	@echo "  make dev        - Runs simulation, server, and frontend concurrently"
	@echo "  make frontend   - Starts the Elm/JS frontend dev server"
	@echo "  make server     - Starts the Python asyncio Game Server"
	@echo "  make simulation - Starts the Python ZMQ Simulation Microservice"
	@echo "  make shared     - Generates the continuous-space assets (Cubemaps)"
	@echo "  make format     - Formats all code across the project"
	@echo "  make clean      - Cleans up generated assets and node_modules"
	@echo "  make test-sim   - Tests the backend Icosahedron mesh and physics engine"

# Start the frontend dev server (runs in watch mode)
frontend:
	@echo "Starting Frontend..."
	cd frontend && nix develop --command $(MAKE) watch

# Start the game server
server:
	@echo "Starting Game Server..."
	cd server && nix develop --command $(MAKE) watch

# Start the simulation microservice
simulation:
	@echo "Starting Simulation Microservice..."
	cd simulation && nix develop --command $(MAKE) watch

# Generate the assets in the shared directory
shared:
	@echo "Generating Shared Assets..."
	cd shared && nix develop --command $(MAKE) all

# Test the backend physics simulation mesh and engine
test-sim:
	@echo "Testing simulation Icosahedron mesh and physics engine..."
	cd simulation && nix develop --command $(MAKE) test

# A helper to run all three in one terminal using simple shell backgrounding
# (Pressing Ctrl+C will kill all)
dev:
	@echo "Starting full dev environment..."
	@trap 'kill %1; kill %2; kill %3' SIGINT; \
		$(MAKE) frontend & \
		$(MAKE) server & \
		$(MAKE) simulation & \
		wait

format:
	cd frontend && nix develop --command $(MAKE) format
	cd server && nix develop --command $(MAKE) format
	cd simulation && nix develop --command $(MAKE) format
	cd shared && nix develop --command $(MAKE) format

clean:
	rm -rf frontend/node_modules
	rm -rf frontend/.parcel-cache
	rm -rf shared/output/*
