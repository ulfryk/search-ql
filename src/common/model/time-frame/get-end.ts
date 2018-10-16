import { ParsedComponents } from 'chrono-node';
import { endOfDay, endOfHour, endOfMinute, endOfMonth, endOfSecond, endOfYear } from 'date-fns';

// tslint:disable-next-line:cyclomatic-complexity
export const getEnd = (components: ParsedComponents): Date => {
  if (!components.isCertain('year')) {
    throw new Error('A date without a year is not a date (at getEnd function).');
  }

  if (!components.isCertain('month')) {
    return endOfYear(new Date(components.get('year')));
  }

  if (!components.isCertain('day')) {
    return endOfMonth(new Date(
      components.get('year'),
      components.get('month') - 1));
  }

  if (!components.isCertain('hour')) {
    return endOfDay(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day')));
  }

  if (!components.isCertain('minute')) {
    return endOfHour(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour')));
  }

  if (!components.isCertain('second')) {
    return endOfMinute(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour'),
      components.get('minute')));
  }

  if (!components.isCertain('millisecond')) {
    return endOfSecond(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour'),
      components.get('minute'),
      components.get('second')));
  }

  return components.date();
};
