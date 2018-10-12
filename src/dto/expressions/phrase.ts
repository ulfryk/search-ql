import { ITermExpression } from './term';

export interface IPhraseExpression extends ITermExpression {
  readonly term: ITermExpression;
}
