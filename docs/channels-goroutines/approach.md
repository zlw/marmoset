# How do we work with channels and goroutines in Marmoset ML?

In Marmoset ML, we can leverage Go's concurrency model by providing built-in support for channels and goroutines. Here's how we can approach this:
## Channels
We can introduce a `channel` type in Marmoset ML that allows for communication between goroutines. Channels can be created with a specific type, and we can provide functions to send and receive data from channels.

```marmoset
ch = channel[int] // create a channel for integers
ch.send(42)       // send an integer to the channel
value = ch.receive() // receive an integer from the channel
```

now this would map directly. but is it a proper approach considering FP nature of Marmoset?
probably now ideal. we might want to have a more FP approach, like tasks, algebraic effects, promises/futures, etc.

taking inspiration from languages like Elm or Haskell, we could implement a more functional approach to concurrency.
example:

```marmoset
task = async {
    value = compute_something()
    return value * 2
} 
result = await(task)
```
but are we losing the ability to have direct channel communication?
not necessarily, we could have both approaches coexisting.
channels for low-level communication, and tasks/promises for higher-level abstractions.

but is there one abstraction that can unify both?
we could consider using algebraic effects to model concurrency.
this way, we can have a unified approach to handle asynchronous computations and communication.
for example:
```marmoset
effect Channel[T] {
    send(value: T): Unit
    receive(): T
}

handler ChannelHandler {
    ch = channel[T]
    
    handle send(value) {
        ch.send(value)
    }
    
    handle receive() {
        return ch.receive()
    }
}
```
this way, we can have a more flexible and composable approach to concurrency in Marmoset ML, while still leveraging Go's powerful concurrency model under the hood.

a working example:
```marmoset
effect Channel[T] {
    send(value: T): Unit
    receive(): T
} 
handler ChannelHandler {
    ch = channel[T]
    
    handle send(value) {
        ch.send(value)
    }
    
    handle receive() {
        return ch.receive()
    }
}

task1 = async {
    ChannelHandler.send(42)
}
task2 = async {
    value = ChannelHandler.receive()
    return value * 2
}
result = await(task2)
```
but how would mutexes work in this scenario? and waiting groups?
we could model mutexes as effects as well:
```marmoset
effect Mutex {
    lock(): Unit
    unlock(): Unit
}
handler MutexHandler {
    mtx = mutex()
    
    handle lock() {
        mtx.lock()
    }
    
    handle unlock() {
        mtx.unlock()
    }
}
```
this way, we can have a consistent approach to concurrency in Marmoset ML, while still leveraging Go's powerful concurrency model under the hood.
we could also model wait groups similarly:
```marmoset
effect WaitGroup {
    add(delta: int): Unit
    done(): Unit
    wait(): Unit
}
handler WaitGroupHandler {
    wg = waitgroup()  
    
    handle add(delta) {
        wg.add(delta)
    }
    handle done() {
        wg.done()
    }
    handle wait() {
        wg.wait()
    }
}
```
this way, we can have a consistent approach to concurrency in Marmoset ML, while still leveraging Go's powerful concurrency model under the hood.
Overall, by leveraging algebraic effects and handlers, we can provide a flexible and composable approachi

im not sure if this is the best approach. im still struggling to se a real world example, with db call, http requests, etc.
example:
```marmoset
effect Http {
    get(url: string): HttpResponse
    post(url: string, body: string): HttpResponse
}
handler HttpHandler {
    handle get(url) {
        return http.get(url)
    }
    handle post(url, body) {
        return http.post(url, body)
    }
}
task = async {
    match HttpHandler.get("https://api.example.com/data") {
        case HttpResponse(200, body) => return body
        case error => error
    }
}
result = await(task)
```
to concurrency in Marmoset ML, while still leveraging Go's powerful concurrency model under the hood
