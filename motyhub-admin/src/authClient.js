import KJUR from 'jsrsasign';
import { AUTH_LOGIN, AUTH_LOGOUT, AUTH_ERROR } from 'admin-on-rest';

export default (type, params) => {
  if (type === AUTH_LOGIN) {
    const { username, password } = params;
    const header = { alg: 'HS256', typ: 'JWT' };
    const payload = { role: username };
    const token = KJUR.jws.JWS.sign(
      header.alg,
      JSON.stringify(header),
      JSON.stringify(payload),
      { utf8: password }
    );
    localStorage.setItem('token', token);
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
