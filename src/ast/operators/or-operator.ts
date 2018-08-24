import { MultiaryOperator } from './multiary-operator';

export class OrOperator extends MultiaryOperator {

  public static readonly one = new OrOperator();

  public toString() {
    return 'or';
  }

}
