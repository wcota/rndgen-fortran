import { withMermaid } from 'vitepress-plugin-mermaid'
import fs from 'node:fs'
import path from 'node:path'
import apiSidebar from '../api/_sidebar.json'

const base = process.env.BASE_PATH || '/rndgen-fortran/'

// Read current project version dynamically from fpm.toml
let version = 'latest'
try {
  const fpmPath = path.resolve(__dirname, '../../fpm.toml')
  const fpmContent = fs.readFileSync(fpmPath, 'utf-8')
  const match = fpmContent.match(/^version\s*=\s*"([^"]+)"/m)
  if (match) version = match[1]
} catch (e) {
  // fallback if file not found
}

const guideSidebar = [
  {
    text: 'Getting Started',
    items: [
      { text: 'Overview & Install', link: '/guide/' },
    ],
  },
  {
    text: 'Guides',
    items: [
      { text: 'Generating Numbers', link: '/guide/usage' },
      { text: 'State & Reproducibility', link: '/guide/state' },
      { text: 'Parallel Streams', link: '/guide/parallel' },
      { text: 'KISS Compatibility', link: '/guide/kiss' },
      { text: 'Migrating from v1', link: '/guide/migrating' },
    ],
  },
  {
    text: 'API Reference',
    items: [
      { text: 'Full Module Index', link: '/api/' },
    ],
  },
]

export default withMermaid({
  title: 'rndgen-fortran',
  description: 'Modern Fortran random number generators centered on xoshiro256**',
  base: base,
  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      'fortran': 'fortran-free-form',
      'f90': 'fortran-free-form',
      'f95': 'fortran-free-form',
      'f03': 'fortran-free-form',
      'f08': 'fortran-free-form',
      'f77': 'fortran-fixed-form',
    },
    config(md) {
      const origRender = md.render.bind(md)
      md.render = (src, env) => {
        // Automatically translate \( ... \) and \[ ... \] into standard MathJax delimiters
        const transformed = src
          .replace(/\\\[([\s\S]*?)\\\]/g, '\n$$\n$1\n$$\n')
          .replace(/\\\(([\s\S]*?)\\\)/g, '$$$1$$')
        return origRender(transformed, env)
      }
    },
  },
  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Guide', link: '/guide/', activeMatch: '^/guide/' },
      { text: 'API Reference', link: '/api/', activeMatch: '^/api/' },
      {
        text: `v${version}`,
        items: [
          { text: 'Releases', link: 'https://github.com/wcota/rndgen-fortran/releases' },
          { text: 'Tags', link: 'https://github.com/wcota/rndgen-fortran/tags' },
        ],
      },
    ],
    sidebar: {
      '/guide/': guideSidebar,
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/wcota/rndgen-fortran' },
    ],
    search: {
      provider: 'local',
    },
    footer: {
      message: 'Released under the LGPL-3.0 License.',
      copyright: 'Copyright © 2018-present Wesley Cota',
    },
  },
  mermaid: {},
  vite: {
    build: {
      target: 'es2022',
    },
  },
})
