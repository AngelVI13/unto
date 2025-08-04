#!/bin/bash

# Initial parameter value
value=40

# Number of iterations
iterations=10

log_file="populate_db.log"

# Loop 10 times
for ((i=1; i<=iterations; i++)); do
  echo "[$(date)] Running ./update_db.sh with parameter: $value"
  ./update_db.sh $value >> $log_file 2>&1

  # Increment value by 20
  value=$((value + 40))

  # Wait for 15 minutes if not the last iteration
  if [[ $i -lt iterations ]]; then
    echo "[$(date)] Waiting 15 minutes before next run..."
    sleep 960  # 16 minutes in seconds
    # sleep 1
  fi
done

echo "[$(date)] Script finished after $iterations iterations."
