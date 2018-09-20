import { IFunctionConfig } from '../config';
import { IExpression } from './expression';

export interface IFunctionExpression extends IExpression<IExpression[]> {
  readonly config: IFunctionConfig;
}
