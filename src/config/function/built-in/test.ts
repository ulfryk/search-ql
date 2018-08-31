import { List, Map } from 'immutable';
import { None } from 'monet';

import { Match, ValueType } from '../../../common/model';
import { FunctionConfig } from '../function-config';

export const testFunction =
  // tslint:disable-next-line:no-unnecessary-callback-wrapper
  new FunctionConfig('test_function', List(), ValueType.Boolean, () => None<Map<string, Match>>());
