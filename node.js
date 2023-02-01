const net = require('net');

const server = net.createServer(socket => {
  console.log('Client connected');
  socket.on('data', data => {
    console.log(`Message received: ${data.toString()}`);
  });
  socket.on('end', () => console.log('Client disconnected'));
});

server.listen(1234, 'localhost', () => console.log('Server listening on localhost:1234'));
