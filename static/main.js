fetch('http://pure-springs-76606.herokuapp.com/options')
  .then(response => response.json())
  .then(json => {
	  console.log(json.options)
  })

fetch('http://pure-springs-76606.herokuapp.com/', {
	method: 'POST',
	body: JSON.stringify({
		required: ['mult', 'div'],
		allowed: { power: true, addition: true }
	}),
	headers: {
		'content-type': 'application/json'
	}
}).then(response => response.json())
.then(json => {
	let problem = json.problem
	document.querySelector('.math-problem').textContent = `\\(${problem}\\)`
	MathJax.typeset()
})

