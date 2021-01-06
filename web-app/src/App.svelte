<script>
	let host = 'http://pure-springs-76606.herokuapp.com'
  if (location.hostname === "localhost") {
		host = 'http://127.0.0.1:8000' 
	}

	import { beforeUpdate, afterUpdate } from 'svelte';

	let options = []
	let problem = ''

	fetch(`${host}/options`)
	.then(response => response.json())
	.then(json => {
		options = json.options.map(name => ({ name, value: false }))
	})

	beforeUpdate(() => {
		let span = document.querySelector('.math-problem')
		if (span != null) {
			span.innerHTML = problem
		}
	})

	afterUpdate(() => {
		if (window.MathJax != null) {
			MathJax.typeset()
		}
	})

	function generate() {
		fetch(host, {
			method: 'POST',
			body: JSON.stringify({
				required: options.filter(op => op.value).map(op => op.name),
				allowed: { power: true, addition: true }
			}),
			headers: {
				'content-type': 'application/json'
			}
		}).then(response => response.json())
			.then(json => {
				problem = `\\(${json.problem}\\)`
			})
	}
</script>

<div class="contents">
  <div class="options">
		{#each options as option}
		<label>
			<input id="option-{option.name}" type="checkbox" bind:checked={option.value} /> <span class="label-body">{option.name}</span>
		</label>
		{/each}
		<button on:click={generate}>generate</button>
	</div>
  <span class="math-problem">{problem}</span>
</div>

