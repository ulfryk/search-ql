import { IExpression } from './expression';

export interface ITermExpression<P> extends IExpression<string> {
  readonly preparedValue: P;
}
