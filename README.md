# ElevatorSimulation

![Alt text](assets/images/elevator_simulation.png?raw=true "Figure 1: Elevator Simulation")

The Elevator Simulation project aims to create a realistic virtual environment that emulates the operation and behavior of multiple elevators in a building. In this project, each component, including elevators, floors, and passengers, is represented as an Akka actor, allowing for efficient and concurrent processing.

The simulation begins by generating a specified number of elevators and assigning them to different floors in a building. Each elevator is programmed to stay on a floor for a duration of one minute, allowing passengers to enter and exit the elevator.  

When a passenger arrives on a floor, they spend a maximum of one minute thinking about their desired destination floor. Once the passenger has made their decision, they enter the elevator that is either already present on their floor or arrives shortly after. The elevators move between floors at a constant speed of one minute per floor, ensuring a realistic representation of elevator travel time.
