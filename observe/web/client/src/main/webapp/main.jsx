import 'unfonts.css';
import './styles/style.scss';
import '/lucuma-css/lucuma-ui-layout.scss';
import '/lucuma-css/lucuma-ui-login.scss';
import '/lucuma-css/lucuma-ui-sequence.scss';
import '/lucuma-css/lucuma-ui-side-tabs.scss';
import '/lucuma-css/lucuma-ui-table.scss';
import '/lucuma-css/lucuma-ui-undo.scss';
import '/lucuma-css/lucuma-ui-variables-dark.scss';
import '/lucuma-css/lucuma-ui-variables-light.scss';
import '/lucuma-css/solar-system.scss';
import 'primereact/resources/primereact.min.css'; // core css
import 'primeicons/primeicons.css'; // icons

import { Main } from '@sjs/main.js';

Main.runIOApp();

if (import.meta.hot) {
  import.meta.hot.accept();
}
