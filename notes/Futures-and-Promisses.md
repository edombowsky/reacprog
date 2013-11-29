These notes were taken from [SCALA INTRODUCTION & FUTURES AND PROMISES](http://arild.github.io/scala-workshop/#/)

# SIP-14 DEFINITION
"A Future is a read-handle to a single value (read-many) that may be available within a specific time-frame" 
<br>
"A Promise is a write-handle to a single value (write-once) that should be made available within a specific time-frame"

# Hello Future
```scala
println("Test print before future")
val s = "hello"

val f = future {
  Thread.sleep(10)
  s + " future!"
}

println("Test print after future")

// Completely asynchronous
f.onSuccess { case s => println(s) }

// Blocks until the future is ready
Await.ready(f, Duration.Inf)
```

# Output
Test print before future <br>
Test print after future  <br>
hello future! <br>

# Basic operations on Futures
```scala
//Asynchronously processes the value in
//the future once the value becomes available.
def foreach[U](f: T => U): Unit

//Creates a new future by applying a function to the successful
//result of this future.
def map[S](f: T => S): Future[S]

//Creates a new future by applying a function to the successful 
//result of this future, and returns the result of the function
//as the new future.
def flatMap[S](f: T => Future[S]): Future[S]

//Creates a new future by filtering the value of the current future
//with a predicate.
def filter(p: T => Boolean): Future[T]
```

# Error Handling
```scala
val riskyRes = future { riskyWork() }
val safeRes = future { safeWork() }

val finalRes = riskyRes recoverWith {
  case e: IllegalArgumentException => safeRes
}
```

# Futures are Composeable
```scala
val keys = future { readFile("keys.txt") }
val values = future { readFile("values.txt") }

val data = keys.zip(values)

val hashMap = data.map((ls: (List[String], List[String])) => {
  ls._1.zip(ls._2).toMap
})

hashMap.recover {
  case e: FileNotFoundException => {
    Map[String, String]()
  }
}.onSuccess {
  case map => {
    println(map)
  }
}

Await.result(hashMap, Duration.Inf)
```

# Producer Consumer Example
```scala
val p = promise[T]
val f = p.future

val producer = future {
  val result = produceSomething()
  p success result
  continueDoingSomethingUnrelated()
}

val consumer = future {
  startDoingSomething()
  f onSuccess {
    case res => doSomething(res)
  }
}
```
