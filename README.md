# PSU.Ada.GameEngine

https://github.com/users/apostasy/projects/1/views/1

# Getting Started
## Initial Setup
### Alire
Download from https://alire.ada.dev/ and install

After installation, run `alr version` to ensure alr has been added to system PATH/Environment Variables

### Visual Studio Code
Download from https://code.visualstudio.com/ and install

### Clone the repo

`git clone mailto:git@github.com:apostasy/PSU.Ada.GameEngine.git`

## Running the application

The command `alr build` will build the project.

The command `alr run` will run the project

## Entity Component System Diagram


```mermaid
classDiagram 
direction 

namespace ecs {
    class Entity {
        +ID
        +Components[] Components
    }
    class Component

    class Transform {
        +float X
        +float Y
        +float Rotation
    }

    class System

}

namespace ecs-renderer {

    class Polygon

    class PolygonDrawer {
        -Generate_Polygon_Vertices(Sides, Radius, Center_X, Center_Y, Rotation)
        -Draw_Regular_Polygon(Image, Polygon)
    }

}

namespace ecs-physics {

    class Thrust {
        +Float X
        +Float Y
    }

    class Mover {
        -ApplyThrust(Transform, Thrust)
    }

}

Component <|.. Thrust
Mover -- Thrust
Mover -- Transform

<<interface>> System

Entity --* Component
Component <|.. Transform
note for Polygon "Polygon is currently an array of Points, thinking of consolidating Points into transform, but it might make sense to keep separate"
Transform *.. Polygon

System ..|> PolygonDrawer
System ..|> Mover

PolygonDrawer -- Polygon

```