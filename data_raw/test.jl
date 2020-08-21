### A Pluto.jl notebook ###
# v0.11.7

using Markdown
using InteractiveUtils

# ╔═╡ ca3d7c7e-e2ba-11ea-10a7-035583e2d4ad


# ╔═╡ 920f088e-e2ba-11ea-3e4b-a1ba9219af80
begin
	n = 10;
	m = 3
	A = rand(1:5,n,m)
	b = rand(100:20:200,n)
end

# ╔═╡ cda7f448-e2ba-11ea-0ad8-99a2231fe6cc
begin
	doc"""
	
		betahat(X, y)
	
	Estimates coefficients of a linear regression model using the `X\y`.
	
	# Arguments
	- `X`: `n` by `k` numeric array.
	- `y`: `n` by 1 numeric array.
	
	# Examples
	```julia-repl
	julia> n = 10
	julia> m = 3
	julia> A = rand(1:5,n,m)
	julia> b = rand(100:20:200,n)
	julia> betahat(X = A, y = b)
	```
	
	"""
	
	function betahat(X, y) 
		return X\y
	end
end

# ╔═╡ 18ef23ae-e2bb-11ea-3302-79415e4dfc19
betahat(A, b)

# ╔═╡ 7541a2da-e2bd-11ea-2740-65d1f4f062f0
b

# ╔═╡ Cell order:
# ╠═ca3d7c7e-e2ba-11ea-10a7-035583e2d4ad
# ╠═920f088e-e2ba-11ea-3e4b-a1ba9219af80
# ╠═cda7f448-e2ba-11ea-0ad8-99a2231fe6cc
# ╠═18ef23ae-e2bb-11ea-3302-79415e4dfc19
# ╠═7541a2da-e2bd-11ea-2740-65d1f4f062f0
