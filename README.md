# Microblogging Platform in Erlang

## Intro 
The task at hand was to model a scalable system using Erlang’s processes and message passing without using Erlang databases. This READMe explains the design  choices made in order to achieve this goal.

## Design Overview

To design this system, we borrowed concepts used in some scalable server systems. The server as provided in the example implementation will maintain a list of its own users. The server will maintain a list of messages sent from all the subscriptions of users in other servers. That is, if user A is subscribed to user B, and user B is in a different server, whenever user B sends a message, it is broadcast to all the servers with subscribers to user B. I decided to spawn multiple servers and assign users to the servers using a hash  unction. As such, every server will also maintain a list of the other servers and whenever it receives a request from a user not in its own server it will  forward it to the appropriate server using the hash function. The hash function assigns users to servers using their name. The hash function looks like this:

```
hash_users(UserName, ServersList) when length(ServersList) > 0 ->
Hash = binary:decode_unsigned(crypto:hash(sha256, UserName)), % returns a number
ServerIndex = (Hash rem length(ServersList)) + 1,
Server = lists:nth(ServerIndex, ServersList),
Server.

```

This function will take in a user’s name and a list of servers and return the appropriate server to assign that user. Each server has 5 core functions it will perform. Figure 1 showcases the typical interaction between 2 users on 2 different servers.

![User Interaction](system_design.png)

• Register a user
• Follow a user
• Send a message
• Let a user see their timeline
• Get the profile of a user

### Register a User
To register a user, any server can receive the message to register a user. If the hash function agrees that the user can be stored on that server, then the list of users on that server is updated.
However, if the user is not on that server, then the register user message is passed to the server specified by the hash user function.

### Following a User
When a user A wants to follow another user B, the can send the follow message request to any server and the message will be routed to the server with user A. That server will then update the
list of users with the new information about the list of users that user A follows. Furthermore, I implemented an inverse follow relationship such that, whenever user A follows user B, then user
B also follows user A. A message is sent to the server with user B to implement the inverse follow relation

### Sending a Message
Whenever a server receives a send message request, it will either service it if the user is in that server or it will forward it to the appropriate server. In servicing the message, it will add it to the user’s list of messages and then go through the followers that user has and broadcast it to their servers. This ensures that all the subscriber’s servers have the latest messages.

### Get Timeline
As mentioned earlier, every server will maintain a list of messages sent to it by other servers with subscribers. To get the timeline of a user A, the server will filter these messages using the
names of the users that A is subscribed to and return them sorted by the time received. If the user is not on the specified server, the request is then forwarded to the server with that user.

### Get Profile
o get the profile of a user, all we have to do is check if the server has that user and if it does, we sort that user’s messages and return that. If not we forward the request to the server that has that user. The way this system is implemented means that more servers allows the system to handle more users concurrently by spawning more server processes to handle the load. The hash function, if we assume true random distribution, allows for even distribution of the users across the servers.