# Cats-Study

## About

Project housing various samples for typelevel cats. Visit https://typelevel.org/cats for more information on the project

## License

This project is under the MIT License.  Cats seems to also be under the MIT 
License with attribution to the license made for Scalaz

## Running

To run, select a test to run and in sbt run the commands:

### From the Terminal

`sbt reload update`

`sbt testOnly com.xyzcorp.<TEST-NAME>`

or to select which test:

`sbt testOnly com.xyzcorp.<TEST-NAME> -- -z '<SUBSTRING>'`

for example,

`sbt testOnly com.xyzcorp.FunctorSpec -- -z 'Case 1:'`

### Within SBT

`;reload;update`

`testOnly com.xyzcorp.<TEST-NAME>`

or to select which test with a substring:

`testOnly com.xyzcorp.<TEST-NAME> -- -z '<SUBSTRING>'`

for example,

`testOnly com.xyzcorp.FunctorSpec -- -z 'Case 1:'`
