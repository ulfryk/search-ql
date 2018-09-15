import { IExpression } from './expression';

export interface IFunctionExpression extends IExpression<IExpression[]> {
  readonly name: string;
}
