import random

import networkx as nx
import matplotlib.pyplot as plt


def manhattan_distance(p1, p2):
    """Computes the Manhattan distance between two points."""
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def generate_sparse_complete_graph(grid_size: int, node_density: float) -> (nx.Graph, dict):
    """
    Generates a complete graph with a subset of grid points as nodes.

    :param grid_size: Size of the NxN grid.
    :param node_density: Fraction of total grid points to be used as nodes (0 to 1).
    :return: Complete graph with nodes, Manhattan distance weights, and node positions.
    """
    all_positions = [(x, y) for x in range(grid_size) for y in range(grid_size)]

    # Sample a subset of positions based on density
    num_nodes = max(2, int(node_density * len(all_positions)))  # At least 2 nodes
    sampled_positions = set(random.sample(all_positions, num_nodes))

    # Ensure an even number of nodes
    if len(sampled_positions) % 2 != 0:
        # Pick a new position that is NOT already in sampled_positions
        available_positions = set(all_positions) - sampled_positions
        new_position = random.choice(list(available_positions))
        sampled_positions.add(new_position)

    # Assign (x, y) positions to node indices
    node_positions = {i: pos for i, pos in enumerate(sampled_positions)}

    # Create a complete graph
    G = nx.complete_graph(len(node_positions))

    # Assign Manhattan distances as edge weights
    for u, v in G.edges():
        G[u][v]['weight'] = manhattan_distance(node_positions[u], node_positions[v])

    return G, node_positions


def find_min_weight_perfect_matching(G: nx.Graph):
    """
    Computes the minimum weight perfect matching using the Blossom algorithm.

    :param G: The input graph.
    :return: A set of edges forming the minimum weight perfect matching and the total matching weight.
    """
    # Negate edge weights to transform the problem into max-weight matching
    for u, v in G.edges():
        G[u][v]['weight'] *= -1

    # Compute maximum weight matching (which minimizes original weights)
    matching = nx.algorithms.matching.max_weight_matching(G, maxcardinality=True, weight='weight')

    # Compute total matching weight (restore original weights for calculation)
    total_matching_weight = -sum(G[u][v]['weight'] for u, v in matching)

    return matching, total_matching_weight


def visualize_graph(G, node_positions, matching):
    """
    Visualizes the graph with nodes, edges, and highlights the minimum weight perfect matching.

    :param G: The graph.
    :param node_positions: Dictionary of node positions.
    :param matching: The set of edges forming the minimum weight perfect matching.
    """
    plt.figure(figsize=(8, 8))

    # Convert node_positions dictionary to a format NetworkX can use
    pos = {node: (x, y) for node, (x, y) in node_positions.items()}

    # Draw all edges in light gray
    nx.draw(G, pos, with_labels=False, node_color='lightblue', edge_color='lightgray', alpha=0.3)

    # Highlight the matching edges in red
    nx.draw_networkx_edges(G, pos, edgelist=matching, edge_color='red', width=2)

    # Draw nodes on the grid
    nx.draw_networkx_nodes(G, pos, node_size=100, node_color='blue')

    # Label nodes with their coordinates
    labels = {node: f"{x},{y}" for node, (x, y) in node_positions.items()}
    nx.draw_networkx_labels(G, pos, labels, font_size=8, font_color="black")

    plt.title("Minimum Weight Perfect Matching (MWPM) on Sparse Grid")
    plt.xlabel("X Coordinate")
    plt.ylabel("Y Coordinate")
    plt.grid(True)
    plt.show()


def save_matching_to_file(filename: str, matching_weight: int, node_positions: dict):
    """
    Saves the minimum weight perfect matching value and node coordinates to a file.

    :param filename: Name of the output file.
    :param matching_weight: The total weight of the minimum weight perfect matching.
    :param node_positions: Dictionary of node positions.
    """
    with open(filename, 'w') as f:
        # Write the total MWPM weight on the first line
        f.write(f"{matching_weight}\n")

        # Write node coordinates, one per line
        for node, (x, y) in node_positions.items():
            f.write(f"{x},{y}\n")

# For testing
if __name__ == "__main__":
    grid_size = 10  # Change this to modify the NxN grid size
    node_density = 0.3  # Adjust how many grid points are chosen as nodes (0.0 to 1.0)
    output_file = "mwpm_output.txt"

    # Step 1: Generate sparse complete graph
    graph, positions = generate_sparse_complete_graph(grid_size, node_density)

    # Step 2: Compute Minimum Weight Perfect Matching (MWPM)
    matching, mwpm_value = find_min_weight_perfect_matching(graph)

    # Step 3: Save results to file
    save_matching_to_file(output_file, mwpm_value, positions)

    # Step 4: Visualize the graph and matching
    visualize_graph(graph, positions, matching)

    print(f"Minimum Weight Perfect Matching Value: {mwpm_value}")
    print(f"Results saved to {output_file}")
