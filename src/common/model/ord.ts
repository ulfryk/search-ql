import { ISetoid } from '@samwise-tech/core';

const enum Ordering { Lt = -1, Eq = 0, Gt = 1 }

interface IOrd<T> extends ISetoid {
  compare(other: IOrd<T>): Ordering;
}

const compareNum = (a: number, b: number) =>
  // tslint:disable-next-line:strict-boolean-expressions
  (a - b) / Math.abs(a - b) as Ordering || Ordering.Eq;

export { compareNum, IOrd, Ordering };
