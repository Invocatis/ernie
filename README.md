# ernie

Ernie provides a simplified, human readable format for creating testing flows that integrate with JVM languages.

## Usage

FIXME: explanation

    $ java -jar ernie-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Description

Ernie breaks regression testing into two pieces: the code that executes the changes, and a script that assembles changes into a flow. We can think of the former as an assemble of lego blocks, that are assembled into testing structures in the latter.

### Legos

Testing flows are oft described as a collection of givens, followed by a list of steps, with verification intermixed, and perhaps some housekeeping afterwards. We can represent this as three discrete functions: Action, Verify, and Clean.

Action is the effect on the system that is being tested. Perhaps a change to the database, a web call, or a simple state change. Actions results are cached in ernie by their parameters, and are thusly considered state idempotent; their effects only happen once. Idempotency should not be guaranteed from within the action, as we will see later, there may be cases where we want to force multiple executions.

The signature of the action function should take any arguments passed by the script, and return the result of the action, which will be used in the latter steps.

Verify is where the effects of the action are tested to have happened. In this step, the state of the entire system should be checked as well as the validity of the result of the action. For instance, if we are testing an entry to a database, we should both check that the instance is created, and the result of the action is as expected.

The signature of the verify function should first take the result of the action, and then any arguments passed by the script and return a boolean. Verification should not have any effects on the state of the system. Exception percolated out of the verification step will be considered a failure in the action.

Clean is where any and all housekeeping is taken. As rarely a single test execution is desired, we want to put our environment state back in a place where latter tests can execute as if on a fresh system. Following our previous examples, here we should remove the entry that was inserted to the database.

The signature of the clean function should first take the result of the action, followed by any arguments passed by the script, with a void return. Cleaning is guaranteed to execute only once within a test, and thus the internals can work however needed.

## The Assembly

Given an assemble of action blocks, we can construct our test flows.

## An Example

Let's run through an example of how one might write a full testing suite in ernie. Consider a simple database application, that handles entries of people. Let's look at some simple CRUD tests. Do not be alarmed, there will be a large amount of hand waving on how the internal system might work.

```java

public class DatabaseActions {

  @Action("insert-person")
  public Person insertPerson(String table, Map<String, Object> params) {

    Person person = new Person()
                        .setFirstName(params.get("first-name"))
                        .setLastName(params.get("last-name"));

    return db.insert(person);
  }

  @Verify("insert-person")
  public boolean verifyInsert(Person entry, Map<String, Object> params) {

    if (entry == null)
      return false;

    Person fromDb = db.readById(Person.class, entry.getId());

    return entry.equals(fromDb)
      && fromDb.getFirstName().equals(params.get("first-name"))
      && fromDb.getLastName().equals(params.get("last-name"));
  }

  @Clean("insert")
  public void cleanInsert(Person person, Map<String, Object> params) {

    db.deleteById(Person.class, person.getId());
  }

}

```

Here we have defined a single action, insert-person, with a verify and clean function associated with it. Let's define a case to invoke it.

```
case person(first-name, last-name):
  ! insert-person({first-name: first-name, last-name: last-name})
```

Let's break this down: we are creating a test case that takes two paramters, a first name, and a last name, and then invokes our java code with those two parameters in a map. The map states the field of first-name should be bound to the value of first-name, being the argument. This highlights an import feature, that field names are not resolved by parameters.

Now look at the invocation: the single bang (!) tells us we're doing a soft invoke, that is, if this action has already been executed on the given parameters, we will just return the case instead of double executing it. If we wanted to force is, we would put a double-bang (!!) to tell ernie we really want it done.

Let's run this test case:

```
? person("Henry", "Winkler")
```

### The Blocks



## Java Action Examples



## Test Examples



...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
