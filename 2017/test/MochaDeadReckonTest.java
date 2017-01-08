package test;

/*
 * Created by izzielau on 1/6/2017.
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.MochaCalibration;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.LightSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TwoWheelDirectDriveDeadReckon;

@Autonomous(name = "TEST Dead Reckon", group = "TEST")
public class MochaDeadReckonTest extends Robot {

    public static final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    public static final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    public static final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    public static final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;

    DcMotor leftFront;
    DcMotor leftRear;
    LightSensor rightLight;

    TwoWheelDirectDriveDeadReckon alignWithLine;
    LightSensorCriteria whiteLineRightCriteria;

    @Override
    public void handleEvent(RobotEvent e)
    {

    }


    @Override
    public void init()
    {
        leftFront = hardwareMap.dcMotor.get("motorFL");
        leftRear = hardwareMap.dcMotor.get("motorBL");

        rightLight = hardwareMap.lightSensor.get("lightRight");
        rightLight.enableLed(true);

        alignWithLine = new TwoWheelDirectDriveDeadReckon(this, TICKS_PER_INCH, TICKS_PER_DEGREE, leftFront, leftRear);
        alignWithLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 15, -0.65);
        alignWithLine.reverseRight();

        whiteLineRightCriteria = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        whiteLineRightCriteria.setThreshold(0.65);
    }

    @Override
    public void start()
    {
        addTask(new DeadReckonTask(this, alignWithLine, whiteLineRightCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Path finished");
                } else if (event.kind == EventKind.SENSOR_SATISFIED) {
                    RobotLog.i("163 Sensor satisfied");
                }
            }
        });
    }
}
