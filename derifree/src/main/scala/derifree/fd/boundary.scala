package derifree.fd

enum BoundaryCondition:
  case Linear
  case Dirichlet(spot: Double, value: Double)
