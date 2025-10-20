/** @type {import("stylelint").Config} */
export default {
  extends: ['stylelint-config-standard-scss'],
  plugins: ['stylelint-value-no-unknown-custom-properties', 'stylelint-color-format'],
  rules: {
    'color-format/format': {
      format: 'hsla',
    },
    'csstools/value-no-unknown-custom-properties': [
      true,
      {
        importFrom: ['vars.css', 'ui/css/target/lucuma-css/dark-theme.css'],
      },
    ],
    'no-descending-specificity': null,
  },

  reportInvalidScopeDisables: true,
  reportNeedlessDisables: true,
  reportUnscopedDisables: true,
};
