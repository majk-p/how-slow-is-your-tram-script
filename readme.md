# Code example: How slow is your tram?

This repository holds code for my talk "How slow is your tram".

It consists of few components:

 * Two api clients for different data sources, `MpkWrocApiClient` is more reliable
 * `Vehicles` algebra - adaptation layer to make the rest of the script independent of selected API
 * `StatsCalculator` that uses the `Vehicles` to obtain and summarize the data using `fs2.Stream`
 * `main.scala` file that configures and runs the process


## Requirements

Make sure you have [scala-cli](https://scala-cli.virtuslab.org/) installed.

## Usage

Run `scala-cli main.scala`

## Notice

This is just an educational project that aims to present a use case for [fs2](https://fs2.io). The calculations are simplified and might not be accurate. 

