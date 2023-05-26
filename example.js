// From https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial
// This work is licensed under the Creative Commons
// Attribution-ShareAlike 2.5 Generic License. To view a copy of this
// license, visit http://creativecommons.org/licenses/by-sa/2.5/ or
// send a letter to Creative Commons, PO Box 1866, Mountain View, CA
// 94042, USA.
var canvas = document.createElement('canvas');
canvas.width = 600;
canvas.height = 300;
canvas.style = "border: 1px solid";
document.body.appendChild(canvas);
const context = canvas.getContext("2d");
let animation;
let running = false;

const ball = {
    x: 100,
    y: 100,
    vx: 5,
    vy: 1,
    radius: 25,
    color: "blue",
    draw() {
        context.beginPath();
        context.arc(this.x, this.y, this.radius, 0, Math.PI * 2, true);
        context.closePath();
        context.fillStyle = this.color;
        context.fill();
    },
};

function clear() {
    context.fillStyle = "rgba(255, 255, 255, 0.3)";
    context.fillRect(0, 0, canvas.width, canvas.height);
}

function draw() {
    clear();
    ball.draw();
    ball.x += ball.vx;
    ball.y += ball.vy;

    if (ball.y + ball.vy > canvas.height || ball.y + ball.vy < 0) {
        ball.vy = -ball.vy;
    }
    if (ball.x + ball.vx > canvas.width || ball.x + ball.vx < 0) {
        ball.vx = -ball.vx;
    }

    animation = window.requestAnimationFrame(draw);
}

canvas.addEventListener("mousemove", (e) => {
    if (!running) {
        clear();
        ball.x = e.clientX;
        ball.y = e.clientY;
        ball.draw();
    }
});

canvas.addEventListener("click", (e) => {
    if (!running) {
        animation = window.requestAnimationFrame(draw);
        running = true;
    }
});

canvas.addEventListener("mouseout", (e) => {
    window.cancelAnimationFrame(animation);
    running = false;
});
