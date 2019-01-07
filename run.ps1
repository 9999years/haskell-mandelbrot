# compile
ghc -O Main

If($?) {
	"Finished compiling! :)"
	$sw = [Diagnostics.Stopwatch]::StartNew()
	.\main.exe
	$sw.Stop()
	"Generation took $($sw.Elapsed.TotalSeconds) seconds"
	ii (ls *.png | sort lastwritetime -Descending)[0]
}
