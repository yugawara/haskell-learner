from functools import reduce
from typing import List, Tuple

# A functor is a mapping from one category to another that preserves the structure of the original category
class Functor:
    def __init__(self, objects: List[Tuple[int, int]], morphisms: List[Tuple[int, int, int]]):
        self.objects = objects
        self.morphisms = morphisms

    # Implement the mapping from morphisms to elements of the codomain
    def __call__(self, morphism):
        # Find the source and target objects of the morphism
        source = next(x[0] for x in self.objects if x[1] == morphism[0])
        target = next(x[0] for x in self.objects if x[1] == morphism[2])
        # Return the composition of the morphisms from the source to the target
        return reduce(lambda acc, m: m[2](acc),
                      [m for m in self.morphisms if m[0] == source and m[1] == target],
                      morphism[1])

# The Yoneda Embedding
def yoneda_embedding(category):
    return [Functor(category[0], category[1]) for obj in category[0]]

def example():
    # Define a simple category with two objects and three morphisms
    objects = [(0, 0), (1, 1)]
    morphisms = [(0, lambda x: x + 1, 1), (0, lambda x: x * 2, 1), (1, lambda x: x - 1, 0)]
    category = (objects, morphisms)

    # Embed the category into its own functor category
    embedded_category = yoneda_embedding(category)

    # Use the functor to map a morphism to an element of the codomain
    f = embedded_category[0]
    print(f((0, lambda x: x + 1, 1))) # 2
