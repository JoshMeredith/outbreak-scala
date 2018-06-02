class Node(
    data:  NodeData,
    world: World
);

class NodeData(
    populations:   Populations,
    nodeIndex:     Int,
    incomingPaths: IndexedSeq[Path],
    outgoingFlow:  Float
);

class Populations(
    susceptible: Float,
    exposed:     Float,
    infected:    Float,
    recovered:   Float
);
