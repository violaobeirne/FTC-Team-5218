package opmodes;

import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadReckonPath;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 12/19/2019.
 */
public class AutomatedLiftTask extends RobotTask {

    public enum LiftStates {
        // needs to be changed to lift states -- implemented state machine in time slice
        // needs enum, switch statement w/ case labls = num values, and set of instructions
        LIFT,
        EXTEND,
        DROP,
        RETRACT,
        LOWER,
        STOPPED,
    }

    protected DcMotor vLift;
    protected CRServo hLift;
    protected Servo claw;
    protected double V_LIFT_POWER;
    protected double H_LIFT_POWER;
    protected double OPEN_CLAW;
    protected int level;
    protected int levelEncoderValue;
    protected LiftStates state;

    public AutomatedLiftTask(Robot robot, DcMotor vLift, double V_LIFT_POWER, CRServo hLift, double H_LIFT_POWER, double OPEN_CLAW, int levelEncoderValue)
    {
        super(robot);

        this.vLift = vLift;
        this.V_LIFT_POWER = V_LIFT_POWER;
        vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        this.hLift = hLift;
        this.H_LIFT_POWER = H_LIFT_POWER;
        this.claw = claw;
        this.OPEN_CLAW = OPEN_CLAW;
        this.levelEncoderValue = levelEncoderValue;
        this.level = 0;
        state = LiftStates.STOPPED;
    }

    @Override
    public void start()
    {
    }

    @Override
    public void stop()
    {
        robot.removeTask(this);
    }

    public void doStoneStack()
    {
        state = LiftStates.LIFT;
    }

    @Override
    public boolean timeslice() {

        switch (state) {
            case LIFT:
                // run vertical lift motor up, check some encoder value multiplier, state = extend, turn off motor
                vLift.setPower(V_LIFT_POWER);
                if  (vLift.getCurrentPosition() % levelEncoderValue == 0) {
                    vLift.setPower(0.0); // doesn't this need to be 0? would setTarget hold the motor?
                    state = LiftStates.EXTEND;
                }
               break;
            case EXTEND:
                // run horizontal CR servo out, check for limit switch, state = drop, turn off servo
                hLift.setPower(H_LIFT_POWER);
                // limit switch goodness
                /*
                if(limit switch) {
                    hLift.setPower(0.0);
                    state = LiftStates.DROP;
                }
                 */
                break;
            case DROP:
                // run to claw position, state = retract
                claw.setPosition(OPEN_CLAW);
                state = LiftStates.RETRACT;
            case RETRACT:
                // run horizontal CR servo in, limit switch criteria, state = lower, turn off horizontal servo
                hLift.setPower(-H_LIFT_POWER);
                /*
                    if(limit switch goodness) [
                        hLift.setPower(0.0);
                        state = LiftStates.LOWER;
                    }
                 */
                break;
            case LOWER:
                // turn on vertical lift motor, check for encoder value is current level * encoder level value, state = retract, turn off motor, level = 0
                vLift.setPower(-V_LIFT_POWER);
                if (vLift.getCurrentPosition() == 0) {
                    vLift.setPower(0.0);
                    vLift.setMode(DcMotor.RunMode.RESET_ENCODERS);
                    state = LiftStates.STOPPED;
                }
                level++;
                break;
            case STOPPED:
                break;
        }

        return false;
        /*
        CURRENT UNDERSTANDING: (DON'T JUDGE)
            - if done reason == something, queue Event if segement is null -- ??? application of segments
            - switch segment states
            - telemetry

         */
    }

}
