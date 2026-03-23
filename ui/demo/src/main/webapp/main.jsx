import 'primereact/resources/primereact.min.css';
import './styles.scss';
import '@lucuma-css/lucuma-ui-sequence.scss';
import '@lucuma-css/lucuma-ui-prime.scss';

import { Demo } from "@sjs/main.js";
Demo.runIOApp()

if (import.meta.hot) {
  import.meta.hot.accept();
}
