.PHONY: help dev frontend backend workshop clean reorganize

# Default target
help:
	@echo "So It Begins - Root Makefile"
	@echo ""
	@echo "Available commands:"
	@echo "  make dev        - Runs both backend and frontend concurrently (requires 'make' or similar process manager)"
	@echo "  make frontend   - Starts the Elm/JS frontend dev server"
	@echo "  make backend    - Starts the Python asyncio backend"
	@echo "  make workshop   - Generates the continuous-space assets (Cubemaps)"
	@echo "  make format     - Formats all code across the project"
	@echo "  make clean      - Cleans up generated assets and node_modules"

# Start the frontend dev server (runs in watch mode)
frontend:
	@echo "Starting Frontend..."
	$(MAKE) -C frontend watch

# Start the backend server
backend:
	@echo "Starting Backend..."
	$(MAKE) -C backend watch

# Generate the assets in the workshop
workshop:
	@echo "Generating Workshop Assets..."
	cd workshop && nix shell --impure --expr '(builtins.getFlake "nixpkgs").legacyPackages.x86_64-linux.python312.withPackages (ps: with ps; [ rasterio pillow numpy ])' --command python generate_cubemap.py

# Test the backend physics simulation mesh and engine
test-sim:
	@echo "Testing backend Icosahedron mesh and physics engine..."
	cd backend/simulation && nix shell --impure --expr '(builtins.getFlake "nixpkgs").legacyPackages.x86_64-linux.python312.withPackages (ps: with ps; [ numpy ])' --command bash -c "python test_mesh.py && python test_3d_physics.py"

# A helper to run both in one terminal using simple shell backgrounding
# (Pressing Ctrl+C will kill both)
dev:
	@echo "Starting full dev environment..."
	@trap 'kill %1; kill %2' SIGINT; \
		$(MAKE) frontend & \
		$(MAKE) backend & \
		wait

format:
	$(MAKE) -C frontend format
	$(MAKE) -C backend format

clean:
	rm -rf frontend/node_modules
	rm -rf frontend/.parcel-cache
	rm -rf workshop/output/*
