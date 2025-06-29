# pj-2025

[Build Status](https://github.com/GuilhermeCunha79/pj-2025/actions/workflows/ci.yml/badge.svg)
[Scala 3](https://www.scala-lang.org/) | [MIT License](LICENSE)

## Description

pj-2025 is a **task scheduling framework** written in **Scala 3**. It provides:

* Modeling of orders, products, and tasks using **opaque types** for stronger type safety
* Definition of task precedence within the same product
* Efficient allocation of physical and human resources, prioritizing shorter tasks with lower demand
* Generation of full schedules (`generateSchedule`) and batch scheduling (`scheduleNextBatch`)
* Automated tests to validate precedence rules, resource constraints, and output integrity

## Scheduling Process

The scheduling engine reads one or more **input files** that define:

1. **Resources**: Available physical and human resources, with types, capacities, and availability windows.
2. **Products**: Definitions of products, each with a sequence of tasks and their inherent precedence order.
3. **Orders**: Customer orders referencing products, specifying required quantities, deadlines, and priority levels.

These input files should be provided in XML format and should follow the templates in the `files/` directory. During execution, the scheduler:

1. **Parses** the resource, product, and order files into domain model instances (`Domain.Resource`, `Domain.Product`, `Domain.Order`).
2. **Validates** that all referenced task and resource IDs exist and that precedence constraints (task A before task B) are consistent.
3. **Allocates** tasks to resources by:

   * Prioritizing orders by deadline and priority
   * Scheduling shorter tasks first when multiple orders compete for the same resource
   * Respecting resource capacity and availability windows
4. **Generates** a complete timeline (`Schedule`) mapping each task to a time slot and resource assignee.
5. **Outputs** the final schedule as a human-readable report or as JSON/CSV in the `files/` directory for further processing.

## Repository Structure

```
pj-2025/
├── docs/                   # Documentation, diagrams, and specifications
├── files/                  # Input examples (JSON, CSV) and output schedules
├── project/                # Additional SBT configuration
├── src/
│   ├── main/
│   │   └── scala/          # Main source code
│   │       └── schedule/   # Domain definitions and implementations
│   └── test/
│       └── scala/          # Test suites using ScalaTest and ScalaCheck
├── build.sbt               # SBT settings (Scala version, dependencies, WartRemover)
├── .gitignore
└── README.txt
```

## Documentation

* High-level domain details and diagrams in `docs/`
* Sample payloads and expected outputs in `files/`

## Contributing

1. Open an issue to suggest improvements or report bugs.
2. Submit a pull request with clear changes and accompanying tests.
3. Follow the existing code style (WartRemover, Scalafmt).

## License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for details.
