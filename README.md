# Simulating Coastal Household Migration in Response to Climate Change

**City focus:** Cartagena  
**Status:** Modelo presentado **01-10-2025**  
**License:** MIT

This repository contains an **Agent-Based Model (ABM)** implemented in **NetLogo** to explore how coastal households may respond to sea-level increase. The model focuses on **Cartagena, Colombia**, using geospatial information (flood risk, infrastructure, socioeconomic groups) to simulate household relocation.

---

# Model Overview

- Purpose: The model aims to understand patterns of household relocation under climate-driven flood risk scenarios.

- Agents: Households are represented as agents with different socioeconomic groups and access to infrastructure.

- Space: The spatial environment is based on raster and shapefile data, representing flood risk and urban structure.

- Dynamics:

    - Agents do not move in cycles. Each household relocates only once, directly to its new location, based on spatial criteria.

    - After all households have moved (or failed to move), the simulation ends.

---

# Interesting NetLogo Features

- The model does not rely on iterative relocation cycles, but instead performs a single-step relocation process.

- For experiments, users can take advantage of BehaviorSpace to automate runs.

    - After each run, export functions can be called to generate outputs.

    - ⚠️ By default, NetLogo overwrites output files when stored in the same folder. To avoid this, users should move results into a different folder after each run

---

# How to Run Experiments

1. Open the model in NetLogo.

2. Configure parameters (e.g., Expansio_Type, RunMode, not_available).

3. Use BehaviorSpace to run experiments:

    - Set desired number of runs and parameter sweeps.

    - Add export commands (e.g., export-matrix-groups, porcentages) at the end of each run.

    - Ensure unique file naming to prevent overwriting.

4. Collect outputs for analysis.

---

## License

This project is released under the MIT License. See the LICENSE file for details.

## Credits
- **Author:** SebFelOlmos  
- **Author:** crcruzr  
- **Author:** Nicolas Arías  



