import { AUTH_LOGIN, AUTH_LOGOUT, AUTH_ERROR } from 'admin-on-rest';

export default (type, params) => {
  if (type === AUTH_LOGIN) {
    const { username, password } = params;
    localStorage.setItem('token', password);
    return Promise.resolve();
  }
  if (type === AUTH_LOGOUT) {
    localStorage.removeItem('token');
    return Promise.resolve();
  }
  if (type === AUTH_ERROR) {
    const { status } = params;
    if (status === 401 || status === 403) {
      localStorage.removeItem('token');
      return Promise.reject();
    }
    return Promise.resolve();
  }
  return Promise.resolve();
}
