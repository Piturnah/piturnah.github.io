<script>
  import GitHubLogo from "./github-mark.svelte";
</script>

<div
  class="flex items-center justify-between border-b border-grey-600 bg-grey-200 px-20 py-3 font-bold"
>
  <a class="text-[1.35rem] text-yellow hover:text-pink" href="/">
    piturnah<span class="text-blue">.xyz</span>
  </a>
  <nav>
    <ul
      class="flex h-full items-center justify-between gap-20 text-xl text-grey-600"
    >
      <li class="hover:text-pink"><a href="/">About</a></li>
      <li class="hover:text-pink">
        <a href="https://github.com/Piturnah">Portfolio</a>
      </li>
      <li class="hover:text-pink"><a href="/">LD45</a></li>
      <li class="hover:text-pink"><a href="/">Tetris</a></li>
      <li class="hover:text-pink">
        <a href="https://github.com/Piturnah/piturnah.github.io">
          <GitHubLogo />
        </a>
      </li>
    </ul>
  </nav>
</div>
