import { ISetoid } from '@samwise-tech/core';

const enum Ordering { Lt = -1, Eq = 0, Gt = 1 }

interface IOrd<T> extends ISetoid {
  compare(other: IOrd<T>): Ordering;
}

const compareNum = (a: number, b: number) => (a - b) / Math.abs(a - b) as Ordering;

export { compareNum, IOrd, Ordering };
