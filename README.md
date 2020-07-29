# Sprinkler 
Simple equation based solver for modeling circuits, using node voltage like equations.
Can be used for eletric, hydraulic, air and moment equations, but currently
only hydraulic equations were fully tested.

Long term goal is to allow mixed modeling between domains with independent solving and
rendering and reporting. Similar to modellica

## Syntax:
  - DSL for grid description
## Geometry:
  - Spatial construction from DSL to 3D coordinate
## Solvers:
  - Non linear solver with gradient
  - WIP: FEM solver for surfaces and volumes on triangular meshes
## Domains:
  - Fluid node-head equations (Hydraulic and Air)
  - WIP: Circuit node-voltage equations (Eletric systems)
  - WIP: Statics Force-moment equations  (Support for surfaces and volumes)
  - WIP: Heat difusion equations (Thermal systems)
## Graphics:
 - Mecha (openscad)
 - DXF
## Reporting:
 - CSV
