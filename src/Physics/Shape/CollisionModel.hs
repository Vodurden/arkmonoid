module Physics.Shape.CollisionModel where

import Physics.Shape.Types

import Linear.V2

movement :: CollisionModel -> V2 Float
movement (DynamicAABB _ m) = m
movement (StaticAABB _) = (V2 0 0)

box :: CollisionModel -> AABB
box (DynamicAABB aabb _) = aabb
box (StaticAABB aabb) = aabb
