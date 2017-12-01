package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.MochaCalibration;
import team25core.AlignWithWhiteLineTask;
import team25core.OpticalDistanceSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;
import team25core.TwoWheelDirectDrivetrain;

@Autonomous(name = "TEST FindTwoBeacons", group = "AutoTest")
public class LameingoFindBothBeaconsTest extends Robot {

    private DcMotor right;
    private DcMotor left;
    private OpticalDistanceSensorCriteria leftSeeWhite;
    private OpticalDistanceSensorCriteria leftSeeBlack;
    private OpticalDistanceSensorCriteria rightSeeWhite;
    private OpticalDistanceSensorCriteria rightSeeBlack;
    private TwoWheelDirectDrivetrain drivetrain;
    private LightSensor leftLight;
    private LightSensor rightLight;
    private AlignWithWhiteLineTask alignTask;

    private static final double LIGHT_MIN = 0.7;
    private static final double LIGHT_MAX = 4.0;
    private static final double PIVOT_MOD = 4.0;

    @Override
    public void handleEvent(RobotEvent e)
    {
        RobotLog.i("Received event " + e.toString());

    }

    @Override
    public void init()
    {
        leftLight = hardwareMap.opticalDistanceSensor.get("left");
        rightLight = hardwareMap.opticalDistanceSensor.get("right");
        right = hardwareMap.dcMotor.get("rightMotor");
        left = hardwareMap.dcMotor.get("leftMotor");

        leftSeeWhite = new OpticalDistanceSensorCriteria(leftLight, OpticalDistanceSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        leftSeeBlack = new OpticalDistanceSensorCriteria(leftLight, OpticalDistanceSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);
        rightSeeWhite = new OpticalDistanceSensorCriteria(rightLight, OpticalDistanceSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        rightSeeBlack = new OpticalDistanceSensorCriteria(rightLight, OpticalDistanceSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);

        leftSeeBlack.setName("Left");
        leftSeeWhite.setName("Left");
        rightSeeBlack.setName("Right");
        rightSeeWhite.setName("Right");

        drivetrain = new TwoWheelDirectDrivetrain(PIVOT_MOD, right, left);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

    }

    @Override
    public void start()
    {
        addTask(new AlignWithWhiteLineTask(this, 70, drivetrain, leftSeeBlack, leftSeeWhite, rightSeeBlack, rightSeeWhite) {
            @Override
            public void handleEvent(RobotEvent e) {
                AlignWithWhiteLineEvent ev = (AlignWithWhiteLineEvent) e;
                if (ev.kind == EventKind.ALIGNED || ev.kind == EventKind.GOOD_ENOUGH) {
                    RobotLog.i("Aligned with first beacon");
                    doDelay();
                } else {
                    RobotLog.i("Aborted at first beacon");
                }
            }
        });
    }

    public void doDelay()
    {
        addTask(new SingleShotTimerTask(this, 4000) {
            @Override
            public void handleEvent(RobotEvent e) {
                RobotLog.i("Done with delay");
                doSecondBeacon();
            }
        });
    }

    public void doSecondBeacon()
    {
        addTask(new AlignWithWhiteLineTask(this, 58, drivetrain, leftSeeBlack, leftSeeWhite, rightSeeBlack, rightSeeWhite) {
            @Override
            public void handleEvent(RobotEvent e) {
                AlignWithWhiteLineEvent ev = (AlignWithWhiteLineEvent) e;
                if (ev.kind == EventKind.ALIGNED || ev.kind == EventKind.GOOD_ENOUGH) {
                    RobotLog.i("Aligned with second beacon");
                } else {
                    RobotLog.i("Aborted at second beacon");
                }
            }
        });
    }
}
