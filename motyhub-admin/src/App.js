import React from 'react';
import { Admin, Resource } from 'admin-on-rest';
import appRestClient from './appRestClient';
import { BlogList } from './blogs';

const App = () => (
  <Admin restClient={appRestClient('http://localhost:8081')}>
    <Resource name="blogs" list={BlogList} />
  </Admin>
);

export default App;

