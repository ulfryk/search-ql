const path = require('path');
const webpackConfig = require('./webpack.test');

module.exports = (config) => {
  config.set({
    basePath: path.resolve(__dirname, '..'),
    frameworks: ['mocha', 'chai'],
    files: [
      'src/polyfills.ts',
      'src/**/*.spec.ts',
    ],
    preprocessors: { 'src/**/*.ts': ['webpack'] },
    webpack: webpackConfig,
    webpackServer: { noInfo: true, stats: require('./webpack-stats-silent') },
    reporters: ['mocha'],
    port: 9876,
    colors: true,
    logLevel: config.LOG_INFO,
    autoWatch: false,
    browsers: ['PhantomJS'],
    singleRun: true,
    concurrency: 6e6,
  });
};
