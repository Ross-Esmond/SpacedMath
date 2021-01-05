<script>
	import { afterUpdate } from 'svelte';

	let options = []
	let problem = ''

	fetch('http://pure-springs-76606.herokuapp.com/options')
	.then(response => response.json())
	.then(json => {
		options = json.options
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
		problem = `\\(${json.problem}\\)`
	})

	afterUpdate(() => {
		if (window.MathJax != null) {
			MathJax.typeset()
		}
	})
</script>

<div class="contents">
  <div class="options">
		{#each options as option}
		<div>
			<input id="option-{option}" type="checkbox" name="option-{option}" /> <label for="option-{option}">{option}</label>
		</div>
		{/each}
	</div>
  <span class="math-problem">{problem}</span>
</div>
