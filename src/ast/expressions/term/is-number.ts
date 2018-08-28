export const isNumber = (value: string): boolean =>
  Number(value) === parseFloat(value) ||
  // tslint:disable-next-line:radix
  Number(value) === parseInt(value);
