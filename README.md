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

The command `alr run` will run.

## Entity Component System Diagram


```mermaid
classDiagram

    direction LR

    Game --* SceneManager
    SceneManager --* Scene
    Scene --* ecs.entity

    ecs.system ..> ecs.component 
    <<interface>> ecs.system

    ecs.entity --*  ecs.component

    ecs.system <|.. Movement_system
    ecs.system <|.. Collision_system
    ecs.system <|.. UserInput_system
    ecs.system <|.. Render_system

    ecs.component <|-- Color
    ecs.component <|-- UserInput
    ecs.component <|-- Transform
    ecs.component <|-- RigidBody
    ecs.component <|-- Shape

    Movement_system ..> Transform
    Movement_system ..> RigidBody
    
    Collision_system ..> RigidBody
    Collision_system ..> Transform
    Collision_system ..> Shape

    UserInput_system ..> Transform
    UserInput_system ..> UserInput

    Render_system ..> Color
    Render_system ..> Transform
    Render_system ..> Shape


```
