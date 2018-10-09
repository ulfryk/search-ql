import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { FunctionConfig } from '../../function-config';

export const nowFunction =
  new FunctionConfig('now', List([]), None(), Right(ValueType.Date));
