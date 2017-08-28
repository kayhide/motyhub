import React from 'react';
import { Admin, Resource } from 'admin-on-rest';
import appRestClient from './appRestClient';
import authClient from './authClient';
import { BlogList } from './blogs';

const App = () => (
  <Admin restClient={appRestClient('http://localhost:8081')} authClient={authClient}>
    <Resource name="blogs" list={BlogList} />
  </Admin>
);

export default App;

