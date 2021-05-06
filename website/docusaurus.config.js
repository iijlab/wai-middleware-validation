/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'wai-middleware-validation',
  tagline: 'WAI Middleware to validate the request and response bodies',
  url: 'https://iij-ii.github.io',
  baseUrl: '/wai-middleware-validation/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'iij-ii', // Usually your GitHub org/user name.
  projectName: 'wai-middleware-validation', // Usually your repo name.
  themeConfig: {
    navbar: {
      title: 'wai-middleware-validation',
      items: [
        {
          href: 'https://github.com/iij-ii/wai-middleware-validation',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      copyright: `Copyright © ${new Date().getFullYear()} IIJ Innovation Institute Inc. Built with Docusaurus.`,
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          routeBasePath: '/',
          sidebarPath: require.resolve('./sidebars.js'),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
