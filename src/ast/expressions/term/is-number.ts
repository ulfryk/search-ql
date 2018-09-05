export const isNumber = (value: string): boolean =>
  Number(value.trim()) === parseFloat(value.trim()) ||
  // tslint:disable-next-line:radix
  Number(value.trim()) === parseInt(value.trim());
