import { MultiaryOperator } from './multiary-operator';

export class AndOperator extends MultiaryOperator {

  public static readonly one = new AndOperator();

  public toString() {
    return 'and';
  }

}
