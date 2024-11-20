# Project 3

## Group Members
### Raj Vora - 35551411
### Rushil Patel - 66999320

## Problem Definition

The goal of this project is to implement the Chord protocol and a simple object access service to prove it's usefulness using Erlang and Actor Model.

## Compile

> `> erl ` <br>
> `> c(chord).` <br>
> `> c(peer). ` <br>

##  Execute (erlang shell)
> `> erl` <br>
> `> chord:main([Nodes, Requests]). `

Where `Nodes` is the number of peers to be created in the peer-to-peer system and `Requests` is the number of requests each peer has to make. When all the peers complete that many requests, the program will exit. Each peer sends a request/second.

## Working

We managed to run Chord protocol for maximum 10000 nodes and 100 messages

![1](images/100-10.png) <br>
![3](images/1000-10.png) <br>
![4](images/5000-10.png) <br>
![5](images/10000-10.png) <br>
![6](images/1000-100.png) <br>
![7](images/5000-100.png) <br>
![8](images/10000-100.png) <br>

## Some Observations

- Average Hops primarily depends on the Number of Nodes doesn't really depend on the Number of Requests.

![Graph1](images/graph1.png) <br>

- Number of Requests mainly elongates the running time of the program and adds some random noise to the output and nothing else.
  
![Graph2](images/graph2.png) <br>
  
- As the number of nodes increases, average hops also increase but in a logarithmic fashion



![Graph3](images/graph3.png) <br>

## 