echo "Setting up the env.."
export PYTHONPATH=/usr/lib/python3.9/site-packages
if [ -d ".soitbegins-env" ]; then
  echo "Skipping env creation as already exists."
else
  echo "Creating venv.."
  python -m venv .soitbegins-env
fi
bash
