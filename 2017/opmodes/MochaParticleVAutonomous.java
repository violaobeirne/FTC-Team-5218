package opmodes;

/*
 * Created by izzielau on 12/17/2016.
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

@Autonomous(name = "(S) Vortex (G) Particle", group = "AutoTest")
public class MochaParticleVAutonomous extends Robot {

    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;

    private int paddleCount = 0;

    private final static double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final static double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;

    private double brushSpeed = 0.5;

    private DcMotorController mc;
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PeriodicTimerTask ptt;
    private FourWheelDirectDriveDeadReckon moveAwayFromWallReckon;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (paddleCount >= 40) {
            ptt.stop();
            stopShooter();
        }
        if (e instanceof PeriodicTimerTask.PeriodicTimerEvent) {
            RobotLog.i("163: Period timer task expired, %d", paddleCount);
            paddleCount++;

            if (paddleCount == 8) {
                // ptt.changeTimeout(50);
            }

            addTask(scoreCenterEncoderTask);
        }
    }

    @Override
    public void init()
    {
        paddleCount = 0;

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        sbod = hardwareMap.dcMotor.get("brush");

        mc = hardwareMap.dcMotorController.get("mechanisms");

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        shooterLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        sbod.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        sbod.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        moveAwayFromWallReckon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveAwayFromWallReckon.addSegment(DeadReckon.SegmentType.STRAIGHT, 7, -0.45);

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 1, brushSpeed);
        ptt = new PeriodicTimerTask(this, 500);

    }

    protected void startShooter()
    {
        shooterLeft.setPower(SHOOTER_VORTEX);
        shooterRight.setPower(-SHOOTER_VORTEX);
    }

    protected void stopShooter()
    {
        shooterLeft.setPower(0);
        shooterRight.setPower(0);
    }

    @Override
    public void start()
    {
        initialMove();
    }

    protected void initialMove() {
        addTask(new DeadReckonTask(this, moveAwayFromWallReckon) {
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent)e;
                switch(event.kind) {
                    case PATH_DONE:
                        startShooter();
                        addTask(new SingleShotTimerTask(robot, 2000) {
                            @Override
                            public void handleEvent(RobotEvent e)
                            {
                                robot.addTask(ptt);
                                robot.addTask(scoreCenterEncoderTask);
                            }
                        });
                        break;
                    default:
                        RobotLog.e("163 Unknown event kind");
                        break;
                }
            }
        });
    }
}
