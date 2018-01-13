package test;

import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import team25core.SensorCriteria;

/**
 * Created by Alexandra on 11/14/2017.
 */

public class AlexandraRangeSensorCriteria implements SensorCriteria {
    private double max;

        DistanceSensor range;

        public AlexandraRangeSensorCriteria(DistanceSensor range, int max)
        {
            this.range = range;
            this.max = max;
        }

        @Override
        public boolean satisfied()
        {
            double distance = range.getDistance(DistanceUnit.CM);
            RobotLog.i("251 Distance %f", distance);

            if (distance <= max) {
                return true;
            } else {
                return false;
            }
        }
    }



