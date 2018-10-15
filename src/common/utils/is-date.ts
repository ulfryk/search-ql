import { isValid } from 'date-fns';

export const isDate = (value: string): boolean =>
  /^[a-z0-9:\-\/,\s\.]+$/ig.test(value) && isValid(new Date(value));
