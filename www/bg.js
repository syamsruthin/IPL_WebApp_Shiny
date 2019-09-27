setTimeout(function () {
	var bat = document.querySelector('#batsman div');
	var ball = document.querySelector('#bowler div');
	bat.addEventListener('click', compareText);
	ball.addEventListener('click', compareText);
	
}, 3000);

function compareText() {
	alert('changed');
}

