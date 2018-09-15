import { ITermExpression } from './term';

export interface IPhraseExpression<T> extends ITermExpression<string> {
  readonly term: ITermExpression<T>;
}
