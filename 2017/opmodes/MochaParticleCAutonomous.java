package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/19/2016.
 */
@Autonomous(name = "(S) Corner (G) Particle", group = "5218")
public class MochaParticleCAutonomous extends Robot {

    private DcMotorController mc;
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;
    private int paddleCount;

    private final static double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final static double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;

    private RunToEncoderValueTask scoreCenterDeadReckonTask;
    private PeriodicTimerTask ptt;

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
                ptt.changeTimeout(50);
            }

            addTask(scoreCenterDeadReckonTask);
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

        scoreCenterDeadReckonTask = new RunToEncoderValueTask(this, sbod, 1, .7);
        ptt = new PeriodicTimerTask(this, 350);

    }

    protected void startShooter()
    {
        shooterLeft.setPower(SHOOTER_CORNER);
        shooterRight.setPower(-SHOOTER_CORNER);
    }

    protected void stopShooter()
    {
        shooterLeft.setPower(0);
        shooterRight.setPower(0);
    }

    @Override
    public void start()
    {
        startShooter();
        this.addTask(new SingleShotTimerTask(this, 3000) {
            @Override
            public void handleEvent(RobotEvent e) {
                robot.addTask(ptt);
                robot.addTask(scoreCenterDeadReckonTask);
            }
        });
    }
}
