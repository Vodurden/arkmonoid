{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Physics.CollisionDetection.Types where

import Physics.Types
import Physics.Shape.Types

import Control.Lens
import Data.Map.Strict (Map)
import Linear.V2

-- | Represents the depth of penetration between two colliding shapes.
-- |
-- | When added to the position of the colliding shape this will resolve
-- | the collision
type PenetrationVector = V2 Float

-- | Represents distance travelled in pixels
type Distance = Float

-- | A ray, used for ray tracing.
type Ray = Segment

-- | Represents the results of tracing a ray against some other physical object.
data RaytraceCollision = RaytraceCollision
  { _point             :: Point    -- ^ The point where the ray intersects the target
  , _segment           :: Segment  -- ^ The segment on the target that was hit
  , _distance          :: Distance -- ^ The distance from the origin of the ray to the target
  , _remainingDistance :: Distance -- ^ The distance from the target to the end of the ray
  } deriving Show
makeFieldsNoPrefix ''RaytraceCollision

-- | Represents a collision that is already occurring this frame.
-- |
-- | If the penetrationVector is applied to the collider this collision will be resolved
data ExistingCollision = ExistingCollision
  { _object :: PhysicalObject -- ^ The object that collided
  , _target :: PhysicalObject -- ^ The object that was collided with
  , _penetrationVector :: PenetrationVector
  } deriving Show
makeFieldsNoPrefix ''ExistingCollision

-- | Represents a collision that will occur this frame between two objects.
data ImpendingCollision = ImpendingCollision
  { _object  :: PhysicalObject
  , _target  :: PhysicalObject
  , _objectLocation :: Point -- ^ The point at which the object will be colliding
  , _targetLocation :: Point -- ^ The point at which the target will be colliding
  , _segment  :: Segment -- ^ A segment representing the surface we are colliding with
  , _objectDistance :: Distance -- ^ The distance from the object to the point of collision
  , _targetDistance :: Distance -- ^ The distance from the target to the point of collision
  } deriving Show
makeFieldsNoPrefix ''ImpendingCollision

-- | Represents a collision between an object and the boundary
data BoundaryCollision = BoundaryCollision
  { _object :: PhysicalObject
  , _penetrationVector :: PenetrationVector
  } deriving Show
makeFieldsNoPrefix ''BoundaryCollision

-- | Represents a collision between an object and a boundary with an additional
-- | identifier that allows the external game engine to understand what object
-- | collided.
data GameBoundaryCollision id = GameBoundaryCollision
  { _objectId :: id
  , _boundaryCollision :: BoundaryCollision
  } deriving Show
makeFieldsNoPrefix ''GameBoundaryCollision

-- | Represents an existing collision between two objects with an additional identifier
-- | that allows the external game engine to understand what two objects
-- | are colliding.
data GameExistingCollision id = GameExistingCollision
  { _objectId :: id
  , _targetId :: id
  , _existingCollision :: ExistingCollision
  } deriving Show
makeFieldsNoPrefix ''GameExistingCollision

-- | Represents an impending collision between two objects with an additional identifier
-- | that allows the external game engine to understand what two objects
-- | are colliding.
data GameImpendingCollision id = GameImpendingCollision
  { _objectId :: id
  , _targetId :: id
  , _impendingCollision :: ImpendingCollision
  } deriving Show
makeFieldsNoPrefix ''GameImpendingCollision

data GameCollision id = GBoundaryCollision id  [GameBoundaryCollision id]
                      | GExistingCollision id  [GameExistingCollision id]
                      | GImpendingCollision id (GameImpendingCollision id)

type GameCollisions id = Map id (GameCollision id)
